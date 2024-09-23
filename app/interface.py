import pygame
import random
    
# Inicialización de Pygame
pygame.init()

# Configuración de pantalla
SCREEN_WIDTH = 800
SCREEN_HEIGHT = 600
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Juego de Tanques")

# Colores
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)

# Tamaño de los bloques (tanques, muros)
BLOCK_SIZE = 40

# Cargar imágenes
player_image = pygame.image.load('images/tank_player.png')
enemy_image = pygame.image.load('images/tank_enemy.png')
wall_image = pygame.image.load('images/wall.png')
# object_enemy = pygame.image.load('images/object_enemy.png') ##Estos son los objetos que vamos a elimninar, una vez se eliminan el juego termina
# Cargar imagen de fondo
background_image = pygame.image.load('images/background.png')
# Ajustar el tamaño del fondo a la pantalla
background_image = pygame.transform.scale(background_image, (SCREEN_WIDTH, SCREEN_HEIGHT))


# Escalar imágenes si es necesario
player_image = pygame.transform.scale(player_image, (BLOCK_SIZE, BLOCK_SIZE))
enemy_image = pygame.transform.scale(enemy_image, (BLOCK_SIZE, BLOCK_SIZE))
wall_image = pygame.transform.scale(wall_image, (BLOCK_SIZE, BLOCK_SIZE))
object_enemy_image = wall_image = pygame.transform.scale(wall_image, (BLOCK_SIZE, BLOCK_SIZE))

clock = pygame.time.Clock()

# El juego soporta una matriz de 20x15 bloques
MAP = [
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1],
    [1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
]


# --- CLASES ---

class Tank:
    def __init__(self, x, y, image, speed=5):  # Agregar parámetro speed
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)
        self.image = image  # Usamos la imagen en lugar del color
        self.original_image = image  # Guardar la imagen original para rotarla después
        self.speed = speed  # Atributo de velocidad
        self.direction = 'UP'  # Dirección inicial

    def draw(self, screen):
        # Dibujar la imagen en lugar del rectángulo
        screen.blit(self.image, self.rect)

    def move(self, x_change, y_change, walls):
        # Nueva posición
        new_rect = self.rect.move(x_change, y_change)

        # Verificar colisión con muros
        for wall in walls:
            if new_rect.colliderect(wall.rect):
                return  # No moverse si choca con un muro

        self.rect = new_rect

    def rotate_image(self):
        """ Rota la imagen del tanque según la dirección """
        if self.direction == 'UP':
            self.image = pygame.transform.rotate(self.original_image, 0)  # Imagen original
        elif self.direction == 'DOWN':
            self.image = pygame.transform.rotate(self.original_image, 180)
        elif self.direction == 'LEFT':
            self.image = pygame.transform.rotate(self.original_image, 90)
        elif self.direction == 'RIGHT':
            self.image = pygame.transform.rotate(self.original_image, -90)

class Wall:
    def __init__(self, x, y):
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)

    def draw(self, screen):
        # Dibujar la imagen del muro
        screen.blit(wall_image, self.rect)


class EnemyTank(Tank):
    def __init__(self, x, y, image, speed=5):
        super().__init__(x, y, image, speed)
        self.last_shot_time = pygame.time.get_ticks()  # Tiempo del último disparo
        self.shoot_interval = 2000  # Intervalo de disparo en milisegundos

    def update(self, player_pos, walls, bullets):
        # Movimiento básico hacia el jugador
        if self.rect.x < player_pos[0]:
            self.image = pygame.transform.rotate(self.original_image, -90)
            x_change = 5
        elif self.rect.x > player_pos[0]:
            self.image = pygame.transform.rotate(self.original_image, 90)
            x_change = -5
        else:
            x_change = 0

        if self.rect.y < player_pos[1]:
            self.image = pygame.transform.rotate(self.original_image, 180)
            y_change = 5
        elif self.rect.y > player_pos[1]:

            y_change = -5
        else:
            y_change = 0

        self.move(x_change, y_change, walls)

        # Disparar hacia el jugador
        current_time = pygame.time.get_ticks()
        if current_time - self.last_shot_time > self.shoot_interval:
            self.shoot(player_pos, bullets)
            self.last_shot_time = current_time

    def shoot(self, player_pos, bullets):
        # Determinar la dirección del disparo
        if player_pos[0] < self.rect.x:
            direction = 'LEFT'
            bullet_x = self.rect.left
            bullet_y = self.rect.centery
        elif player_pos[0] > self.rect.x:
            direction = 'RIGHT'
            bullet_x = self.rect.right
            bullet_y = self.rect.centery
        elif player_pos[1] < self.rect.y:
            direction = 'UP'
            bullet_x = self.rect.centerx
            bullet_y = self.rect.top
        else:
            direction = 'DOWN'
            bullet_x = self.rect.centerx
            bullet_y = self.rect.bottom

        bullet = Bullet(bullet_x, bullet_y, direction, is_enemy_bullet=True)
        bullets.append(bullet)

class Bullet:
    def __init__(self, x, y, direction, is_enemy_bullet=False):
        self.rect = pygame.Rect(x, y, 5, 5)
        self.speed = 10
        self.direction = direction  # ('UP', 'DOWN', 'LEFT', 'RIGHT')
        self.is_enemy_bullet = is_enemy_bullet  # Indica si es una bala enemiga

    def move(self):
        if self.direction == 'UP':
            self.rect.y -= self.speed
        elif self.direction == 'DOWN':
            self.rect.y += self.speed
        elif self.direction == 'LEFT':
            self.rect.x -= self.speed
        elif self.direction == 'RIGHT':
            self.rect.x += self.speed

    def draw(self, screen):
        pygame.draw.rect(screen, BLACK, self.rect)

    def off_screen(self):
        return (self.rect.x < 0 or self.rect.x > SCREEN_WIDTH or
                self.rect.y < 0 or self.rect.y > SCREEN_HEIGHT)


# --- FUNCIONES ---
def generate_walls(map_data):
    """Generar muros según el mapa"""
    walls = []
    for row in range(len(map_data)):
        for col in range(len(map_data[row])):
            if map_data[row][col] == 1:
                wall = Wall(col * BLOCK_SIZE, row * BLOCK_SIZE)
                walls.append(wall)
    return walls

walls = generate_walls(MAP)


# --- FUNCIONES AUXILIARES ---

def handle_bullets(bullets, enemies, walls, player):
    for bullet in bullets[:]:
        bullet.move()

        # Verificar colisión con muros
        for wall in walls:
            if bullet.rect.colliderect(wall.rect):
                if bullet in bullets:
                    bullets.remove(bullet)
                break

        # Verificar colisión con el jugador
        if bullet.rect.colliderect(player.rect):
            # Restablecer la posición del jugador
            player.rect.topleft = (100, 40)  # Posición inicial del jugador
            if bullet in bullets:
                bullets.remove(bullet)  # Eliminar la bala
            break  # Salir del bucle para evitar múltiples eliminaciones

        # Verificar colisión con enemigos
        for enemy in enemies[:]:  # Hacemos una copia de la lista de enemigos para eliminar sin problemas
            if bullet.rect.colliderect(enemy.rect):
                if bullet in bullets:
                    bullets.remove(bullet)  # Eliminar la bala
                enemies.remove(enemy)  # Eliminar el enemigo
                break

        if bullet.off_screen() and bullet in bullets:
            bullets.remove(bullet)




def generate_random_position(walls):
    while True:
        x = random.randint(0, (SCREEN_WIDTH // BLOCK_SIZE) - 1) * BLOCK_SIZE
        y = random.randint(0, (SCREEN_HEIGHT // BLOCK_SIZE) - 1) * BLOCK_SIZE
        new_rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)

        # Verificar si la nueva posición colisiona con algún muro
        if not any(new_rect.colliderect(wall.rect) for wall in walls):
            return (x, y)


# --- CONFIGURACIÓN INICIAL ---

player = Tank(100, 40, player_image)  # Tanque del jugador
#walls = [Wall(200, 200), Wall(240, 200)]  # Lista de muros
enemies = [EnemyTank(*generate_random_position(walls), enemy_image) for _ in range(2)]
bullets = []  # Lista de balas

# --- BUCLE PRINCIPAL ---

running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

        # Disparar bala al presionar la barra espaciadora
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                # Crear una bala en la dirección actual del jugador
                if keys[pygame.K_UP]:
                    bullet = Bullet(player.rect.centerx, player.rect.top, 'UP')
                elif keys[pygame.K_DOWN]:
                    bullet = Bullet(player.rect.centerx, player.rect.bottom, 'DOWN')
                elif keys[pygame.K_LEFT]:
                    bullet = Bullet(player.rect.left, player.rect.centery, 'LEFT')
                elif keys[pygame.K_RIGHT]:
                    bullet = Bullet(player.rect.right, player.rect.centery, 'RIGHT')
                bullets.append(bullet)

        # Manejo del movimiento del tanque del jugador
    keys = pygame.key.get_pressed()
    x_change, y_change = 0, 0

    if keys[pygame.K_UP]:
        y_change = -player.speed
        player.direction = 'UP'
    elif keys[pygame.K_DOWN]:
        y_change = player.speed
        player.direction = 'DOWN'
    elif keys[pygame.K_LEFT]:
        x_change = -player.speed
        player.direction = 'LEFT'
    elif keys[pygame.K_RIGHT]:
        x_change = player.speed
        player.direction = 'RIGHT'

    player.move(x_change, y_change, walls)
    player.rotate_image()

    # Actualizar y mover los tanques enemigos
    for enemy in enemies:
        enemy.update(player.rect.center, walls, bullets)

    # Manejo de las balas
    handle_bullets(bullets, enemies, walls, player)

    # Dibujar todo
    screen.blit(background_image, (0, 0))

    # Dibujar muros
    for wall in walls:
        wall.draw(screen)

    # Dibujar al jugador y enemigos
    player.draw(screen)
    for enemy in enemies:
        enemy.draw(screen)

    # Dibujar las balas
    for bullet in bullets:
        bullet.draw(screen)

    # Actualizar la pantalla
    pygame.display.flip()

    # Controlar el FPS
    clock.tick(30)

pygame.quit()
